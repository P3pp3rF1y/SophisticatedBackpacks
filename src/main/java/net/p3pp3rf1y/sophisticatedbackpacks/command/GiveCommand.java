package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.builder.ArgumentBuilder;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.arguments.EntityArgument;
import net.minecraft.entity.item.ItemEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.SoundEvents;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.registries.ForgeRegistries;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackAccessLogger;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RandHelper;

import java.util.Collection;
import java.util.UUID;

public class GiveCommand {
	private GiveCommand() {}

	@SuppressWarnings("java:S1452")
	static ArgumentBuilder<CommandSource, ?> register() {
		return Commands.literal("give")
				.then(Commands.argument("targets", EntityArgument.players())
						.then(Commands.argument("backpackUuid", BackpackUUIDArgumentType.backpackUuid())
								.executes(context -> giveBackpack(context.getSource(), context.getArgument("backpackUuid", UUID.class), EntityArgument.getPlayers(context, "targets")))
						)
				);
	}

	private static int giveBackpack(CommandSource source, UUID backpackUuid, Collection<ServerPlayerEntity> players) {
		BackpackAccessLogger.getBackpackLog(backpackUuid).ifPresent(alr -> {
			Item item = ForgeRegistries.ITEMS.getValue(alr.getBackpackItemRegistryName());
			ItemStack backpack = new ItemStack(item);
			if (!backpack.getDisplayName().getString().equals(alr.getBackpackName())) {
				backpack.setDisplayName(new StringTextComponent(alr.getBackpackName()));
			}
			backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(backpackWrapper -> {
				backpackWrapper.setColors(alr.getClothColor(), alr.getTrimColor());
				backpackWrapper.setContentsUuid(backpackUuid);
			});

			players.forEach(p -> giveBackpackToPlayer(backpack, p));

			if (players.size() == 1) {
				source.sendFeedback(new TranslationTextComponent("commands.sophisticatedbackpacks.give.success", players.iterator().next().getDisplayName()), true);
			} else {
				source.sendFeedback(new TranslationTextComponent("commands.sophisticatedbackpacks.give.success", players.size()), true);
			}
		});
		return 0;
	}

	private static void giveBackpackToPlayer(ItemStack backpack, ServerPlayerEntity p) {
		boolean flag = p.inventory.addItemStackToInventory(backpack);
		if (flag && backpack.isEmpty()) {
			backpack.setCount(1);
			ItemEntity itemEntity = p.dropItem(backpack, false);
			if (itemEntity != null) {
				itemEntity.makeFakeItem();
			}

			p.world.playSound(null, p.getPosX(), p.getPosY(), p.getPosZ(), SoundEvents.ENTITY_ITEM_PICKUP, SoundCategory.PLAYERS, 0.2F, (RandHelper.getRandomMinusOneToOne(p.getRNG()) * 0.7F + 1.0F) * 2.0F);
			p.container.detectAndSendChanges();
		} else {
			ItemEntity itementity = p.dropItem(backpack, false);
			if (itementity != null) {
				itementity.setNoPickupDelay();
				itementity.setOwnerId(p.getUniqueID());
			}
		}

		ItemEntity itemEntity = p.dropItem(backpack, false);
		if (itemEntity != null) {
			itemEntity.makeFakeItem();
		}
	}
}
