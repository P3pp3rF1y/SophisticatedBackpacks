package net.p3pp3rf1y.sophisticatedbackpacks.compat.curios;

import net.minecraft.entity.LivingEntity;
import net.minecraft.item.Item;
import top.theillusivec4.curios.api.SlotTypePreset;
import top.theillusivec4.curios.api.type.capability.ICurio;

public class CuriosBackpackWrapper implements ICurio {
	private final Item backpackItem;

	public CuriosBackpackWrapper(Item backpackItem) {
		this.backpackItem = backpackItem;
	}

	@Override
	public boolean canEquip(String identifier, LivingEntity livingEntity) {
		return identifier.equals(SlotTypePreset.BACK.getIdentifier());
	}
}
