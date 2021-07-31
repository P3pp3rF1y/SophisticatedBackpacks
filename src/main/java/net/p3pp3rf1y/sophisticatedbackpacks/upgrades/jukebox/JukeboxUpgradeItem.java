package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox;

import net.minecraft.entity.LivingEntity;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.MusicDiscItem;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import javax.annotation.Nullable;
import java.util.UUID;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

public class JukeboxUpgradeItem extends UpgradeItemBase<JukeboxUpgradeItem.Wrapper> {
	public static final UpgradeType<Wrapper> TYPE = new UpgradeType<>(Wrapper::new);

	@Override
	public UpgradeType<Wrapper> getType() {
		return TYPE;
	}

	public static class Wrapper extends UpgradeWrapperBase<Wrapper, JukeboxUpgradeItem> implements ITickableUpgrade {
		private static final int KEEP_ALIVE_SEND_INTERVAL = 5;
		private final ItemStackHandler discInventory;
		private long lastKeepAliveSendTime = 0;
		private boolean isPlaying;

		protected Wrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
			super(backpackWrapper, upgrade, upgradeSaveHandler);
			discInventory = new ItemStackHandler(1) {
				@Override
				protected void onContentsChanged(int slot) {
					super.onContentsChanged(slot);
					upgrade.addTagElement("discInventory", serializeNBT());
					save();
				}

				@Override
				public boolean isItemValid(int slot, ItemStack stack) {
					return stack.getItem() instanceof MusicDiscItem;
				}
			};
			NBTHelper.getCompound(upgrade, "discInventory").ifPresent(discInventory::deserializeNBT);
			isPlaying = NBTHelper.getBoolean(upgrade, "isPlaying").orElse(false);
		}

		public void setDisc(ItemStack disc) {
			discInventory.setStackInSlot(0, disc);
		}

		public ItemStack getDisc() {
			return discInventory.getStackInSlot(0);
		}

		public void play(World world, BlockPos pos) {
			play(world, (serverWorld, backpackUuid) ->
					ServerBackpackSoundHandler.startPlayingDisc(serverWorld, pos, backpackUuid, Item.getId(getDisc().getItem()), () -> setIsPlaying(false)));
		}

		public void play(LivingEntity entity) {
			play(entity.level, (world, backpackUuid) ->
					ServerBackpackSoundHandler.startPlayingDisc(world, entity.position(), backpackUuid, entity.getId(),
							Item.getId(getDisc().getItem()), () -> setIsPlaying(false)));
		}

		private void play(World world, BiConsumer<ServerWorld, UUID> play) {
			if (!(world instanceof ServerWorld) || getDisc().isEmpty()) {
				return;
			}
			backpackWrapper.getContentsUuid().ifPresent(backpackUuid -> play.accept((ServerWorld) world, backpackUuid));
			setIsPlaying(true);
		}

		private void setIsPlaying(boolean playing) {
			isPlaying = playing;
			NBTHelper.setBoolean(upgrade, "isPlaying", playing);
			save();
		}

		public void stop(LivingEntity entity) {
			if (!(entity.level instanceof ServerWorld)) {
				return;
			}
			backpackWrapper.getContentsUuid().ifPresent(backpackUuid ->
					ServerBackpackSoundHandler.stopPlayingDisc((ServerWorld) entity.level, entity.position(), backpackUuid)
			);
			setIsPlaying(false);
		}

		public IItemHandler getDiscInventory() {
			return discInventory;
		}

		@Override
		public void tick(@Nullable LivingEntity entity, World world, BlockPos pos) {
			if (isPlaying && lastKeepAliveSendTime < world.getGameTime() - KEEP_ALIVE_SEND_INTERVAL) {
				backpackWrapper.getContentsUuid().ifPresent(backpackUuid ->
						ServerBackpackSoundHandler.updateKeepAlive(backpackUuid, world, entity != null ? entity.position() : Vector3d.atCenterOf(pos), () -> setIsPlaying(false))
				);
				lastKeepAliveSendTime = world.getGameTime();
			}
		}

		public boolean isPlaying() {
			return isPlaying;
		}
	}
}
