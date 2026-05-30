package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.sounds.SoundSource;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.mixin.MobAccessor;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutFitResult;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutPart;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IClientStorageContentsProvider;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IInventoryLayoutContributor;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IInventorySlotBlocker;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;

import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;

public class MobCatcherUpgradeWrapper extends UpgradeWrapperBase<MobCatcherUpgradeWrapper, MobCatcherUpgradeItem> implements IUpgradeWrapper, ITickableUpgrade, IClientStorageContentsProvider, IInventoryLayoutContributor, IInventorySlotBlocker {
	private static final int MIN_AMBIENT_SOUND_DELAY = 220;
	private static final int AMBIENT_SOUND_DELAY_VARIATION = 260;
	private static final float MUFFLED_AMBIENT_VOLUME = 0.08F;
	private static final float MUFFLED_AMBIENT_VOLUME_VARIATION = 0.03F;
	private static final float MUFFLED_AMBIENT_PITCH = 0.62F;
	private static final float MUFFLED_AMBIENT_PITCH_VARIATION = 0.08F;
	private long nextAmbientSoundTime = Long.MIN_VALUE;

	public MobCatcherUpgradeWrapper(IStorageWrapper storageWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(storageWrapper, upgrade, upgradeSaveHandler);
	}

	public boolean isAdvanced() {
		return upgradeItem.isAdvanced();
	}

	@Override
	public boolean hideSettingsTab() {
		return true;
	}

	@Override
	public boolean canBeDisabled() {
		return false;
	}

	@Override
	public void addClientStorageContents(CompoundTag contents) {
		if (!(storageWrapper instanceof IBackpackWrapper backpackWrapper)) {
			return;
		}

		CompoundTag capturedMobsTag = MobCatcherStorage.getCapturedMobsTag(backpackWrapper);
		if (capturedMobsTag.contains(MobCatcherStorage.CAPTURED_MOBS_TAG)) {
			contents.put(MobCatcherStorage.CAPTURED_MOBS_TAG, capturedMobsTag.get(MobCatcherStorage.CAPTURED_MOBS_TAG));
		}
	}

	@Override
	public boolean isSlotBlocked(int slot) {
		return storageWrapper instanceof IBackpackWrapper backpackWrapper && MobCatcherStorage.isSlotBlocked(backpackWrapper, slot);
	}

	@Override
	public boolean isInventoryLayoutSlotHandled(int slot, int columns) {
		return storageWrapper instanceof IBackpackWrapper backpackWrapper && MobCatcherStorage.isInventoryLayoutSlotHandled(backpackWrapper, slot, columns);
	}

	@Override
	public Optional<InventoryLayoutPart> getInventoryLayoutPart(int slot, int columns, int targetColumns) {
		return storageWrapper instanceof IBackpackWrapper backpackWrapper ? MobCatcherStorage.getInventoryLayoutPart(backpackWrapper, slot, columns, targetColumns) : Optional.empty();
	}

	@Override
	public void applyInventoryLayout(InventoryLayoutFitResult fitResult, int columns) {
		if (storageWrapper instanceof IBackpackWrapper backpackWrapper) {
			MobCatcherStorage.applyInventoryLayout(backpackWrapper, fitResult, columns);
		}
	}

	@Override
	public void clientTick(Entity entity, Level level, BlockPos pos) {
		if (!level.isClientSide || entity == null) {
			return;
		}

		if (!(storageWrapper instanceof IBackpackWrapper backpackWrapper)) {
			return;
		}

		List<CapturedMob> capturedMobs = MobCatcherStorage.getCapturedMobs(backpackWrapper);
		if (capturedMobs.isEmpty()) {
			nextAmbientSoundTime = Long.MIN_VALUE;
			return;
		}

		long gameTime = level.getGameTime();
		if (nextAmbientSoundTime == Long.MIN_VALUE) {
			nextAmbientSoundTime = gameTime + 40 + level.random.nextInt(120);
			return;
		}
		if (gameTime < nextAmbientSoundTime) {
			return;
		}

		nextAmbientSoundTime = gameTime + MIN_AMBIENT_SOUND_DELAY + level.random.nextInt(AMBIENT_SOUND_DELAY_VARIATION);
		CapturedMob capturedMob = capturedMobs.get(level.random.nextInt(capturedMobs.size()));
		getAmbientSound(capturedMob, level).ifPresent(sound -> level.playLocalSound(entity.getX(), entity.getY(), entity.getZ(), sound, SoundSource.PLAYERS,
				MUFFLED_AMBIENT_VOLUME + level.random.nextFloat() * MUFFLED_AMBIENT_VOLUME_VARIATION,
				MUFFLED_AMBIENT_PITCH + (level.random.nextFloat() - 0.5F) * MUFFLED_AMBIENT_PITCH_VARIATION, false));
	}

	private Optional<SoundEvent> getAmbientSound(CapturedMob capturedMob, Level level) {
		return MobCatcherStorage.getEntityType(capturedMob)
				.map(entityType -> entityType.create(level))
				.filter(Mob.class::isInstance)
				.map(Mob.class::cast)
				.map(mob -> {
					mob.load(capturedMob.entityNbt());
					return ((MobAccessor) mob).sophisticatedbackpacks$getAmbientSound();
				});
	}
}
